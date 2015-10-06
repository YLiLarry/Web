import Ember from 'ember';

export default Ember.Controller.extend({
    init: function() {
        this.set('converter', new Markdown.Converter());
    },
    actions: {
        updateMarkdown: function() {
            this.set('previewText', this.get('converter').makeHtml(this.get('markdownText')));
        }
    }
});
