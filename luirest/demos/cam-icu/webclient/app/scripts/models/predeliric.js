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
              },
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
            var internmenttypeid = patient.get('internmenttypeid') || 0;
            var coma = this.get('coma') || 0;
            var apache2 = this.get('apache2') || 0;
            var hassedation = this.get('hassedation') || 0;
            var hasacidosis = this.get('hasacidosis') || 0;
            var urea = this.get('urea') || 0;
            var isurgency = this.get('isurgency') || 0;
            var morphine = this.get('morphine') || 0;
            var hasinfection = this.get('hasinfection') || 0;
            
            var risk = 0.0;
            
            risk += (age * 0.0183) 
            
            if (internmenttypeid === 2) {
                //clinical
                risk += 0.1446
            } else if (internmenttypeid === 3) {
                //trauma
                risk += 0.5316
            } else if (internmenttypeid === 4) {
                //neuro
                risk += 0.6516
            }
            
            if (coma === 2) {
                //medication
                risk += 0.2578
            } else if (coma === 3) {
                //other
                risk +=  1.0721
            } else if (coma === 4) {
                //combination
                risk += 1.3361
            }
            
            risk += (apache2 * 0.0272)
            
            if (hassedation) {
                risk +=  0.6581
            }
            
            if (hasacidosis) {
                risk += 0.1378
            }
            
            risk += (urea * 0.0141)
            
            if (isurgency) {
                risk += 0.1891
            }
            
            if (morphine === 2) {
                //0,01 - 7,1mg
                risk += 0.1926
            } else if (morphine === 3) {
                //7,2 - 18,6mg
                risk += 0.0625
            } else if (morphine === 4) {
                //> 18,6mg
                risk += 0.2414
            }
            
            if (hasinfection) {
                risk += 0.4965
            }
                          
            return risk;              
        }
    });
                       
                          
    return PreDeliricModel;
                          
                          
});
