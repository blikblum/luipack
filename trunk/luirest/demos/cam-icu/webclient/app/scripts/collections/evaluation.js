/*global define*/

define([
    'underscore',
    'backbone',
    'models/evaluation'
], function (_, Backbone, EvaluationModel) {
    'use strict';

    var EvaluationCollection = Backbone.Collection.extend({
        model: EvaluationModel,
        initialize: function(options) {
          this.patient = options.patient;
        },
        url: function(){
          return '/patients/active/' + this.patient.get('id') + '/evaluations'
        }
    });

    return EvaluationCollection;
});
