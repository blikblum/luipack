/*global define*/

define([
  'underscore',
  'backbone',
  'models/evaluation'
], function (_, Backbone, EvaluationModel) {
  'use strict';

  var props = {
    model: EvaluationModel,
    initialize: function (options) {
      this.patient = options.patient;
    },
    url: function () {
      return app.BASE_URL + '/patients/active/' + this.patient.get('id') + '/evaluations'
    }
  };

  //if (app.useLocalStorage)
  //    props.localStorage = new Backbone.LocalStorage("evaluation");

  var EvaluationCollection = Backbone.Collection.extend(props);

  return EvaluationCollection;
});