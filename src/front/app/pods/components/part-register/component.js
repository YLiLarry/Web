import Ember from 'ember';

export default Ember.Component.extend({
    actions: {
        checkUsername: function() {
            if (! this.get('username')) {
                this.set('usernameValid', false);
                this.set('usernameMsg', "请输入用户名");
            } else {
                this.set('usernameValid', true);
                this.set('usernameMsg', "用户名可以使用");
            }
        },
        checkEmail: function() {
            if (! this.get('email')) {
                this.set('emailValid', false);
                this.set('emailMsg', "请输入邮箱");
            } else {
                this.set('emailValid', true);
                this.set('emailMsg', "邮箱可以注册");
            }
        },
        checkPassword: function() {
            if (! this.get('password')) {
                this.set('passwordValid', false);
                this.set('passwordMsg', "请输入密码");
            } else {
                this.set('passwordValid', true);
                this.set('passwordMsg', "密码符合要求");
            }
        },
        checkPassword2: function() {
            if (! this.get('password2')) {
                this.set('password2Valid', false);
                this.set('password2Msg', "请重新输入密码");
            } else
            if (this.get('password2') !== this.get('password')) {
                this.set('password2Valid', false);
                this.set('password2Msg', "密码不一致");
            } else {
                this.set('password2Valid', true);
                this.set('password2Msg', "密码一致");
            }
        },
    }
});
