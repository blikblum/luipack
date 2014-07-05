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
        },

        defaults: {
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

        parse: function(response, options)  {
            return response;
        }
    });

    return PreDeliricModel;
});
