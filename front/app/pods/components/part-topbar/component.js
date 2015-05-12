import Ember from 'ember';

export default Ember.Component.extend({
    didInsertElement: function() {
        this._super();
        Ember.run.scheduleOnce('afterRender', this, function() {
            var Component = this;
            $('body').on('login', function(event, data) {
                Component.set('session', data);
            });
        });
    },
    actions: {
        logout: function() {
            Cookies.remove('session', {path: '/'});
            this.set('session','');
            $('body').trigger('logout');
        }
    }
});