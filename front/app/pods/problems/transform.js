import DS from 'ember-data';

export default DS.Transform.extend({
  deserialize: function(serialized) {
    console.log(serialize);
    return JSON.parse(serialized);
  },

  serialize: function(deserialized) {
    console.log(deserialize);
    return deserialized;
  }
});
