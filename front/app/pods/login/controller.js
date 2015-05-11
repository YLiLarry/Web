import Ember from "ember";

export default Ember.Controller.extend({
    actions: {
        redirectToIndex: function() {
            this.transitionToRoute('/');
        }    
    }
});
