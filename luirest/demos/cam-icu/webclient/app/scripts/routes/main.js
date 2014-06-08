/*global define*/

define([
    'jquery',
    'backbone',
  'models/patient',
  'views/patients',
  'views/patientedit'
], function ($, Backbone, PatientModel, PatientsView, PatienteditView) {
    'use strict';

    var MainRouter = Backbone.Router.extend({
        routes: {
          'patients': 'showPatients',
          'patients/add/:registry': 'addPatient'
        },
      showPatients: function(){
        app.setMainView(new PatientsView({collection: app.data.patients}).render())
      },
      addPatient: function(registry){
        var patient = new PatientModel({registry:registry});
        app.setMainView(new PatienteditView({collection: app.data.patients, model: patient}).render())
      }

    });

    return MainRouter;
});
