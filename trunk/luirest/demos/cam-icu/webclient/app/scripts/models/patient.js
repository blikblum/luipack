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
      getEvaluations: function(callback){
        if (!this._evaluations){
          this._evaluations = new Evaluations({patient: this});
        };
        if (!this._evaluations._fetched){
            this._evaluations.fetch({
                success: function(collection){
                    collection._fetched = true;
                    if (callback){
                        callback(collection);
                    }
                }
            });

        } else {
            if (callback) {
                callback(this._evaluations)
            }
        }

        return this._evaluations;
      }
    });

    return PatientModel;
});
