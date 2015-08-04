import Ember from 'ember';

export default Ember.Route.extend({
    model: function(params) {
        return this.store.findRecord('problems', params.id);
    },
    // renderTemplate() {
    //     console.log("called");
    //     // this.render('problem', { into: 'problems' });
    //     // this.render('problems');
    // }
});
