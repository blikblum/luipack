/*global define*/

define([
  'underscore',
  'backbone',
  'collections/evaluation',
  'models/predeliric'
], function (_, Backbone, Evaluations, PreDeliric) {
  'use strict';

  var PatientModel = Backbone.Model.extend({
    defaults: {},
    initialize: function () {
      this.computedFields = new Backbone.ComputedFields(this);
    },
    computed: {
      age: {
        get: function () {
          return calculateOAAge(this.get('birthdate'))
        },
        depends: ['birthdate'],
        toJSON: false
      },
      evaluationcount: {
        get: function () {
          if (this._evaluations) {
            return this._evaluations.length;
          } else {
            return this.get('evaluationcount')
          }
        },
        depends: [
          function (callback) {
            this._updateEvaluationCount = callback;
                }],
        toJSON: false
      },
      predeliricrisk: {
        get: function () {
          if (this._predeliric) {
            return this._predeliric.get('risk');
          } else {
            return this.get('predeliricrisk')
          }

        },
        depends: [
          function (callback) {
            this._updatePredeliricRisk = callback;
                }],
        toJSON: false
      }
    },
    validation: {
      name: {
        required: true
      },
      bednumber: {
        pattern: 'number'
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
      },
      dischargedate: {
        required: true
      },
      dischargereasonid: {
        required: true
      }
    },
    getEvaluations: function () {
      var self = this;
      var deferred = $.Deferred();
      var evaluations;
      if (this._evaluations) {
        deferred.resolveWith(this, [this._evaluations]);
      } else {
        evaluations = new Evaluations({
          patient: this
        });
        evaluations.fetch({
          success: function (collection) {
            self._evaluations = evaluations;
            self._updateEvaluationCount();
            evaluations.on('add remove reset', self._updateEvaluationCount);
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
    getPreDeliric: function () {
      var self = this;
      var deferred = $.Deferred();
      var predeliric;
      if (this._predeliric) {
        deferred.resolveWith(this, [this._predeliric]);
      } else {
        predeliric = new PreDeliric({
          patient: this
        });
        predeliric.fetch({
          success: function (model) {
            self._predeliric = predeliric;
            self._updatePredeliricRisk();
            predeliric.on('change:risk', self._updatePredeliricRisk);
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