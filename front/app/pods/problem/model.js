import DS from 'ember-data';

export default DS.Model.extend({
    title: DS.attr("string"),
    content: DS.attr("string"),
    answerCount: DS.attr("number"),
    isSolvedByUser: DS.attr("boolean"),
    friendSolutionCount: DS.attr("number"),
    thumbnail: "http://www.intechopen.com/source/html/43273/media/image47.png",
});
