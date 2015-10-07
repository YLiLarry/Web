import Ember from 'ember';

export default Ember.Controller.extend({
    editController: Ember.inject.controller('problem.single.edit'),
    actions: {
        submit: function() {
            alert(this.get('editController').get('markdownText'));
        }
    }
});
