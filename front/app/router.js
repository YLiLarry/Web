import Ember from 'ember';
import config from './config/environment';

var Router = Ember.Router.extend({
    location: config.locationType
});

export default Router.map(function() {
    this.route('login');
    this.route('register');
    this.route('problem', {path: '/problem'}, function() {
        this.route('single.view', {path: '/:id'});
        this.route('single.edit', {path: '/:id/edit'});
        this.route('all', {
            path: '/',
        });
        this.route('invited');
        this.route('recommended');
    });
});
  
