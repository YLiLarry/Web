import DS from 'ember-data';

export default DS.RESTAdapter.extend({
    namespace: "api/v1",
    shouldBackgroundReloadRecord: function() {return true;}
});
