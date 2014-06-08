/*global define*/

define([
    'jquery',
    'backbone',
    'models/patient',
    'views/patients',
    'views/patientedit',
    'views/evaluations'
], function ($, Backbone, PatientModel, PatientsView, PatienteditView, EvaluationsView) {
    'use strict';

    var MainRouter = Backbone.Router.extend({
        routes: {
          'patients': 'showPatients',
          'patients/add/:registry': 'addPatient',
          'patients/:patientid/evaluations': 'showEvaluations',
          'patients/:patientid/edit': 'editPatient'
        },
      showPatients: function(){
        app.setMainView(new PatientsView({collection: app.data.patients}).render())
      },
      addPatient: function(registry){
        var patient = new PatientModel({registry:registry});
        app.setMainView(new PatienteditView({collection: app.data.patients, model: patient}).render())
      },
      showEvaluations: function (patientId) {
          var patient = app.data.patients.get(patientId);
          var view;
          if (patient){
              view = new EvaluationsView({model: patient});
              app.setMainView(view.render());
          } else {
              alert('Paciente com id ' + patientId + ' não encontrado');
              app.mainRouter.navigate('#/patients');
          }

      },
        editPatient: function (patientId) {
            var patient = app.data.patients.get(patientId);
            var view;
            if (patient){
                view = new PatienteditView({model: patient});
                app.setMainView(view.render());
            } else {
                alert('Paciente com id ' + patientId + ' não encontrado');
                app.mainRouter.navigate('#/patients');
            }
        }

    });

    return MainRouter;
});
