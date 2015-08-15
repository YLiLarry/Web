import Ember from 'ember';

export default Ember.Controller.extend({
    queryParams: ['page'],
    page: 1,
    total: Ember.computed(function() {
        var m = this.get('model');
        return (m.get('meta').total / m.get('perpage'));
    })
});
