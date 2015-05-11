import DS from 'ember-data';

export default DS.Model.extend({
  id: DS.attr('number'),
  thumbnail: DS.attr('number'),
});
