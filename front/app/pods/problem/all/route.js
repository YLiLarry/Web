import Ember from 'ember';

export default Ember.Route.extend({
    queryParams: {
        page: { refreshModel: true },
    },
    model: function(params) {
        return this.store.query('problem', {
            current: params.page,
            perpage: 50
        });
    },
    afterModel: function(model) {
        var len = model.get('length');
        model.set('perpage', 50);
        model.forEach(function(elem,idx) {
            if (idx > 0) {
                elem.set('prev', model.objectAt(idx - 1).get('id'));
            }
            if (idx + 1 < len) {
                elem.set('next', model.objectAt(idx + 1).get('id'));
            }
        })
    }
});
