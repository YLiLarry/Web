import Ember from 'ember';
import config from './config/environment';

var Router = Ember.Router.extend({
  location: config.locationType
});

export default Router.map(function() {
  this.route('login');
  this.route('register');
  this.route('problems', function() {
    this.route('invited');
    this.route('recommended');
  });
  this.route('problem', function() {
    this.route('index', {
      path: ':id'
    });
  });
});
