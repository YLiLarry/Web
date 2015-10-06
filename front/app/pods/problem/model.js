import DS from 'ember-data';

export default DS.Model.extend({
    title: DS.attr("string"),
    content: DS.attr("string"),
    answerCount: DS.attr("number"),
    solvedByUser: DS.attr("number"),
    friendSolutionCount: DS.attr("number"),
    next: DS.attr("number"),
    prev: DS.attr("number"),
    thumbnail: "http://www.intechopen.com/source/html/43273/media/image47.png",
});
