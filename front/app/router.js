import Ember from 'ember';
import config from './config/environment';

var Router = Ember.Router.extend({
    location: config.locationType
});

export default Router.map(function() {
    this.route('login');
    this.route('register');
    this.route('problem', {path: '/problem'}, function() {
        this.route('single', {path: ''}, function() {
            this.route('view', {path: '/:id'});
            this.route('edit', {path: ''}, function() {
                this.route('update', {path: '/:id/update'});
                this.route('create', {path: '/create'});
            });
        });
        this.route('all', {path: '/'});
        this.route('invited');
        this.route('recommended');
    });
});
  
