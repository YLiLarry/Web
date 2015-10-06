import DS from 'ember-data';

export default DS.RESTSerializer.extend({
    isNewSerializerAPI: true,
    attrs: {
        id: 'idx',
    },
    modelNameFromPayloadKey: function(name) {
        return name;
    }
});
