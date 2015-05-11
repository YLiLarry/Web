import Ember from 'ember';

export default Ember.Component.extend({
    // didInsertElement: function() {
    //     Ember.run.scheduleOnce('afterRender', this, function() {
    //         $('body').on('login', function() {
    //            console.log('logged!'); 
    //         });
    //     });
    // },
    actions: {
        loadRandomQuestion: function() {
            this.set('loadedQuestion', true);
        },
        login: function() {
            var Component = this;
            
            this.set('invalidEmailCSS', '');
            this.set('invalidPasswordCSS', '');
            
            // validation
            if (! this.get('email')) {
                this.set('invalidEmailCSS', 'has-error');
                this.set('noEmail', true);
                this.set('message', "告诉我你的邮箱好吗?");
                return;
            } else if (! this.get('password')) {
                this.set('invalidEmailCSS', '');
                this.set('invalidPasswordCSS', 'has-error');
                this.set('noPassword', true);
                this.set('message', "小声地告诉我密码好吗?");
                return;
            }
            
            this.set('invalidEmailCSS', '');
            this.set('invalidPasswordCSS', '');
            
            // post
            $.ajax({
                method: "POST",
                url: "/api/v1/login",
                data: {
                    email:    this.get('email'), 
                    password: this.get('password')
                },
                success: function(session) {
                    var sessionObj = JSON.parse(session);
                    Cookies.set('session', session, {path: '/'});
                    $('#part-login').modal('hide');
                    $('body').trigger('login', sessionObj);
                }.bind(this),
                error: function() {
                    this.set('message', "你的用户名或密码无效");
                }.bind(this)
            });
        }
    }
});

