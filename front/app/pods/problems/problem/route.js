import Ember from 'ember';

export default Ember.Route.extend({
    model: function(params) {
        return this.store.findRecord('problems', params.id);
    },
    renderTemplate() {
        this.render('problems');
        this.render('problems.problem', { into: 'problems' });
    }
});
