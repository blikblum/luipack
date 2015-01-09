/*global define*/

define([
  'underscore',
  'backbone',
  'models/patient'
], function (_, Backbone, PatientModel) {
  'use strict';

  var PatientCollection = Backbone.Collection.extend({
    url: app.BASE_URL + '/patients/active',
    model: PatientModel,
    comparator: 'bednumber'
  });

  return PatientCollection;
});