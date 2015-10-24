from urllib.parse import urlencode
from decimal import Decimal

from django.conf import settings
from django.http import HttpResponseRedirect, HttpResponseBadRequest
from django.conf.urls import patterns, url
from django.utils.decorators import method_decorator
from django.core.urlresolvers import reverse
from django.utils.translation import ugettext_lazy as _

from paypalrestsdk import Api as PaypalAPI, Payment as PaypalPayment
import requests as requestlib

from .base import PaymentBackendBase
from shopengine.models import (Order, OrderPayment, ExtraPriceOrderField,
    ExraPriceOrderItemField)
from shopengine.decorators import order_session_required
from shopengine.utils.formatting import moneyfmt


if getattr(settings, 'PAYPAL_PAYMENT_SANDBOX', True):
    paypalmode = 'sandbox'
else:
    paypalmode = 'live'

shopiepaypal = PaypalAPI({
        'mode': paypalmode,
        'client_id': getattr(settings, 'PAYPAL_API_CLIENT_ID', None),
        'client_secret': getattr(settings, 'PAYPAL_API_SECRET', None)
    })

rate_exchange = getattr(settings, 'PAYPAL_RATE_EXCHANGE', 13800)

class PaypalBackendPaymentStandard(PaymentBackendBase):
    url_namespace = 'paypal_payment'
    backend_name = _('Paypal Standard')

    def get_urls(self):
        urlpatterns = patterns('',
                url(r'^$', self.payment_view, name='paypal_payment'),
                url(r'^exec/(?P<payment_key>[^\.]+)/$', self.execute_payment, name='paypal_payment_execute')
            )
        return urlpatterns

    @method_decorator(order_session_required)
    def payment_view(self, request):
        order = Order.objects.get_order_from_request(request)
        absolute_uri = request.build_absolute_uri
        items = self._get_order_items(order)
        amount_total = sum([Decimal(it['price']) for it in items])
        payment_arguments = {
            'intent': 'sale',
            'payer': {
                'payment_method': 'paypal'
            },
            'redirect_urls': {
                'return_url': absolute_uri(reverse('paypal_payment_execute',
                    kwargs={
                        'payment_key': order.order_key
                    })),
                'cancel_url': absolute_uri(reverse('checkout'))
            },
            'transactions': [{
                'item_list': {
                    'items': items
                },
                'amount': {
                    'total': moneyfmt(amount_total),
                    'currency': 'USD'
                },
                'description': 'Make sure to include'
            }]
        }
        payment = PaypalPayment(payment_arguments, api=shopiepaypal)
        if payment.create():
            # set amount to 0.0 first
            transaction_id = payment.id or self._create_transaction_id(order)
            self.create_order_payment(order, transaction_id=transaction_id,
                amount=Decimal('0.0'))
            self.empty_cart(order, request)
            # find the redirect url to let customer approve it
            for link in payment.links:
                if link.method == 'REDIRECT':
                    redirect_url = str(link.href)
                    return HttpResponseRedirect(redirect_url)
        else:
            import django.contrib.messages.api as message_api
            # error
            message_api.warning('Gagal, memproses pembayaran melalui paypal. ' +
                'silahkan coba kembali. :)')
            return HttpResponseRedirect(reverse('cart'))

    def execute_payment(self, request, **kwargs):
        try:
            payer_id = request.GET['PayerID']
            payment_id = request.GET['paymentId']
            order_key = kwargs.get('payment_key', None)
        except KeyError:
            return HttpResponseBadRequest('Bad request.')
        else:
            payment = PaypalPayment.find(payment_id, api=shopiepaypal)
            if payment.execute({'payer_id': payer_id}):
                if order_key is not None:
                    order = Order.objects.get(order_key=order_key)
                    order.update_status(Order.COMPLETED)
                    try:
                        payment_order = OrderPayment.objects.get(order=order)
                    except OrderPayment.DoestNotExist:
                        pass
                    else:
                        payment_order.amount = Decimal(order.order_total)
                        payment_order.save()
                return HttpResponseRedirect(reverse('checkout_thankyou', kwargs={
                        'order_key': order.order_key
                    }))
            else:
                message_api.warning('Gagal, memproses pembayaran melalui paypal ' +
                    'silahkan coba lagi. :)')
                order = Order.objects.get(order_key=order_key)
                return HttpResponseRedirect(reverse('checkout_pay', kwargs={
                        'order_key': order.order_key
                    }))

    def _get_order_items(self, order):
        output = []
        item_ids = []
        for item in order.items.all():
            item_ids.append(item.pk)
            formatted_price = moneyfmt(item.unit_price / Decimal(rate_exchange))
            transaction = {
                'name': item.product.fullname,
                'sku': 'prod-' + item.product.fullname,
                'price': formatted_price,
                'currency': 'USD',
                'quantity': item.quantity
            }
            output.append(transaction)
        # then try to add extra price there
        for eo in ExtraPriceOrderField.objects.filter(order=order):
            formatted_price = moneyfmt(eo.value / Decimal(rate_exchange))
            transaction = {
                'name': eo.label,
                'sku': 'extra-' + eo.label,
                'price': formatted_price,
                'quantity': '1'
            }
            output.append(transaction)
        for eio in ExraPriceOrderItemField.objects.filter(order_item__pk__in=item_ids):
            formatted_price = moneyfmt(eio.value / Decimal(rate_exchange))
            transaction = {
                'name': eio.label,
                'sku': 'extra-item-' + eo.label,
                'price': formatted_price,
                'quantity': '1'
            }
            output.append(transaction)
        return output