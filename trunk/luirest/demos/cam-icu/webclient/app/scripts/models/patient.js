/*global define*/

define([
    'underscore',
    'backbone',
    'collections/evaluation'
], function (_, Backbone, Evaluations) {
    'use strict';

    var PatientModel = Backbone.Model.extend({
        defaults: {
        },
      getEvaluations: function(){
        if (!this._evaluations){
          this.evaluations = new Evaluations({patient: this});
          this.evaluations.fetch();
        }
        return this.evaluations;
      }
    });

    return PatientModel;
});
