import Ember from 'ember';

export default Ember.Route.extend({
    afterModel: function() {
        // Ember.run.scheduleOnce('afterRender', this, function() {
        //     var wall = new freewall('#freewall');
        //     wall.fitWidth();
        // });
    },
    model: function() {
        var id = 1;
        var arr = JSC.array(100, JSC.object({
            title: "标题",
            friendSolutionCount: JSC.integer(5),
            isSolvedByUser: JSC.boolean(),
            thumbnail: "http://www.intechopen.com/source/html/43273/media/image47.png",
            answerCount: JSC.integer(10000),
        }))();
        arr.forEach(function(elem, idx) {
            elem.id = id++;
        });
        return arr;
    }
});
