/*global define*/

define([
    'underscore',
    'backbone'
], function (_, Backbone) {
    'use strict';

    var PreDeliricModel = Backbone.Model.extend({
        url: function (){
            return app.BASE_URL + '/patients/'+ this.patient.get('id')+'/predeliric'
        },
        sync: function(method, model, options) {
            if (method === 'create') {
                method = 'update'
            }
            return Backbone.sync(method, model, options);
        },
        initialize: function(options) {
            this.patient = options.patient;
            this.computedFields = new Backbone.ComputedFields(this);
        },
        defaults: {
        },
        computed: {
            risk: {
              get: function (){
                return this.calculateRisk();
              }
              depends: ['coma', 'apache2', 'hassedation', 'hasacidosis', 'urea', 'isurgency', 'morphine', 'hasinfection']
            }
        },
        validation: {
            coma: {
                required: true
            },
            apache2: {
                required: true,
                pattern: 'number'
            },
            hassedation: {
                required: true
            },
            hasacidosis: {
                required: true
            },
            urea: {
                required: true,
                pattern: 'number'
            },
            isurgency: {
                required: true
            },
            morphine: {
                required: true
            },
            hasinfection: {
                required: true
            }
        },
        calculateRisk: function() {
            var patient = this.patient;              
            var age = patient.get('age') || 0;
            var coma = this.get('coma') || 0;              
            var risk = 0.0;
                          
                          
            return risk;              
        }
    });
                       
                          
    return PreDeliricModel;
                          
                          
});
