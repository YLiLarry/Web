import Ember from 'ember';

export default Ember.Component.extend({
    didInsertElement: function() {
        Ember.run.scheduleOnce('afterRender', this, function() {
            var group = $(this.element).children('.input-group');
            group.find('.form-control').on('focus', function() {
                group.addClass('focused');
            }).on('blur', function() {
                group.removeClass('focused');
            });
        });
    }
});

