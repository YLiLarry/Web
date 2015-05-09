import Ember from 'ember';

export default Ember.Route.extend({
    model: function() {
        var id = 0;
        return JSC.array(100, JSC.object({
            id: id++,
            title: "标题",
            thumbnail: "http://www.intechopen.com/source/html/43273/media/image47.png",
            answerCount: JSC.integer(10000),
        }))();
    }
});
