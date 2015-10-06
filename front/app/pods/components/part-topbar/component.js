import Ember from 'ember';

export default Ember.Component.extend({
    didInsertElement: function() {
        this._super();
        Ember.run.scheduleOnce('afterRender', this, function() {
            var Component = this;
            // $('body').on('login', function(event, data) {
            //     Component.set('session', data);
            // });
            var en = Cookies.get('session');
            if (en) {
                this.set('session', JSON.parse(en));
            }
        });
    },
    actions: {
        logout: function() {
            Cookies.remove('session', {path: '/'});
            Cookies.remove('uid', {path: '/'});
            Cookies.remove('token', {path: '/'});
            this.set('session', undefined);
            // $('body').trigger('logout');
            location.reload();
        }
    }
});
