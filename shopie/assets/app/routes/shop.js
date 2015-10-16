import Ember from 'ember';
import ShortcutsRoute from 'shopie/mixins/shortcuts-route';
import PaginationRoute from 'shopie/mixins/pagination-route';
import styleBody from 'shopie/mixins/style-body';

let paginationSettings = {
    page: 1,
    limit: 20
};

export default Ember.Route.extend(styleBody, ShortcutsRoute, PaginationRoute, {
    titleToken: 'Shop',
    classNames: ['shop', 'js-shop'],

    model: function () {
        return this.store.query('product', paginationSettings).then((result) => {
            let meta = result.get('meta');
            if (meta) {
                this.setCurentMeta(meta);
            }
        });
    },

    setupController: function (controller, model) {
        this._super(controller, model);
        this.setupPagination(paginationSettings);
    },

    setupCurrentMeta: function(meta) {
        this.controller.set('currentMeta', meta);
    },

    scrollContent: function (amount) {
        var content = Ember.$('.js-products'),
            scrolled = content.scrollTop();

        content.scrollTop(scrolled + 50 * amount);
    },

    shortcuts: {
        'v': 'viewProduct'
    }
});
