import Ember from 'ember';

export default Ember.Route.extend({
    model: function(posts) {
        return this.store.query('problems', {
            current: 1,
            perpage: 100
        });
    },
    afterModel: function(route) {
        var ls = route.store.peekAll('problems');
        var len = ls.get('length');
        ls.forEach(function(elem,idx) {
            if (idx > 0) {
                elem.set('prev', ls.objectAt(idx - 1).get('id'));
            }
            if (idx + 1 < len) {
                elem.set('next', ls.objectAt(idx + 1).get('id'));
            }
        })
    }
});
