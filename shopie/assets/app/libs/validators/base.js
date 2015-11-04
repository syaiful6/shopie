import Ember from 'ember';

let BaseValidator = Ember.Object.extend({
    properties: [],
    passed: false,

    check: function(model, prop) {
        this.set('passed', true); //default

        if (prop && this[prop]) {
            this[prop](model);
        } else {
            this.get('properties').forEach((property) => {
                if (this[property]) {
                    this[property](model);
                }
            });
        }
        return this.get('passed');
    },

    invalidate: function () {
        this.set('passed', false)
    }
});

export default BaseValidator;
