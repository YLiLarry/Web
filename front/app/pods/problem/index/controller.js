import Ember from 'ember';

export default Ember.Controller.extend({
    init: function() {
        Ember.run.scheduleOnce('afterRender', this, function() {
            var controller = this;
            $('#file-upload').change(function() {
                var seg = $(this).val().split('\\');
                controller.set('filename', seg[seg.length - 1]);
                var selectedFile = $(this)[0].files[0];
                var Reader = new FileReader();
                Reader.onload = function(){
                    $('#file-content').val(Reader.result);
                };
                Reader.readAsText(selectedFile)
            });
        });
    },
    actions: {
        uploadFile: function() {
            var controller = this;
            var file = new Blob([$('#file-content').val()], {type: 'text/plain'});
            var form = new FormData();
            form.append('problem', this.get('model.id'));
            form.append('file', file, 'q'+this.get('model.id'));
            form.append('language', 'plain');
            // post
            $.ajax({
                method: "POST",
                url: "/api/v1/compile",
                contentType: false,
                processData: false,
                data: form,
                success: function(response) {
                    controller.set('response', response);
                    controller.set('answerCorrent', true);
                }.bind(this),
                error: function(response) {
                    controller.set('response', response.responseText);
                    controller.set('answerCorrent', true);
                }.bind(this)
            });
        }
    }
});
