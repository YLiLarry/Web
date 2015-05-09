import Ember from 'ember';

export default Ember.Route.extend({
    model: function(params) {
        var id = 1;
        return JSC.object({
            id: params.id,
            title: "标题",
            thumbnail: "http://www.intechopen.com/source/html/43273/media/image47.png",
            answerCount: JSC.integer(10000),
            content: JSC.string(JSC.integer(3000), JSC.character()),
            next: parseInt(params.id)+1,
            prev: parseInt(params.id)-1
        })();
    }
});
