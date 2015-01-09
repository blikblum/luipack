/*global define*/

define([
  'underscore',
  'backbone'
], function (_, Backbone) {
  'use strict';

  var EvaluationModel = Backbone.Model.extend({

    initialize: function (options) {

    },

    defaults: function () {
      var now;
      now = new Date();
      now = (now - new Date(1899, 11, 30)) / (24 * 60 * 60 * 1000);
      return {
        date: now
      }
    },

    validation: {
      date: {
        required: true
      },
      deliriumid: {
        required: true
      },
      ventilationid: {
        required: true
      },
      rass: {
        required: true
      },
      shiftid: {
        required: true
      }
      /*,
      sedation: {
        required: true
     }*/
    }
  });

  return EvaluationModel;
});