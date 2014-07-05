/*global define*/

define([
    'underscore',
    'backbone',
    'collections/evaluation',
    'models/predeliric'
], function (_, Backbone, Evaluations, PreDeliric) {
    'use strict';

    var PatientModel = Backbone.Model.extend({
        defaults: {
        },
        initialize: function () {

        },
        //todo: move to computed
        get: function (attr) {
          if (attr === 'evaluationcount') {
              if (this._evaluations) {
                  return this._evaluations.length;
              } else {
                  return this.attributes['evaluationcount']
              }
          } else if (attr === 'predeliricrisk') {
              if (this._predeliric) {
                  return this._predeliric.get('risk');
              } else {
                  return this.attributes['predeliricrisk']
              }
          } else {
           return Backbone.Model.prototype.get.apply(this, arguments);
          }
        },
        validation:{
          name: {
             required: true
          },
          birthdate: {
             required: true
          },
            registry: {
                required: true
            },
        originationid: {
            required: true
        },
            internmentdate: {
                required: true
            },
            internmenttypeid: {
                required: true
            },
            diagnosticid: {
                required: true
            }

        },
      getEvaluations: function() {
          var self = this;
          var deferred = $.Deferred();
          var evaluations;
          if (this._evaluations) {
              deferred.resolveWith(this, [this._evaluations]);
          } else {
              evaluations = new Evaluations({patient: this});
              evaluations.fetch({
                  success: function(collection) {
                      self._evaluations = evaluations;
                      deferred.resolveWith(self, [evaluations]);
                  },
                  error: function () {
                      deferred.rejectWith(self, ['Error fetching evaluations']);
                      console.log('Error fetching evaluations', evaluations)
                  }
              })

          }
          return deferred.promise();
      },
    getPreDeliric: function() {
        var self = this;
        var deferred = $.Deferred();
        var predeliric;
        if (this._predeliric) {
            deferred.resolveWith(this, [this._predeliric]);
        } else {
            predeliric = new PreDeliric({patient: this});
            predeliric.fetch({
                success: function(model) {
                    self._predeliric = predeliric;
                    deferred.resolveWith(self, [predeliric]);
                },
                error: function () {
                    deferred.rejectWith(self, ['Error fetching predeliric']);
                    console.log('Error fetching predeliric', predeliric)
                }
            })

        }
        return deferred.promise();
    }
    });

    return PatientModel;
});
