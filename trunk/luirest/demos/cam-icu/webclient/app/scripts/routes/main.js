/*global define*/

define([
    'jquery',
    'backbone',
    'models/patient',
    'views/patients',
    'views/patientedit',
    'views/evaluations',
    'views/evaluation',
    'models/evaluation',
    'views/predeliric'
], function ($, Backbone, PatientModel, PatientsView, PatienteditView, EvaluationsView, EvaluationView, Evaluation, PreDeliricView) {
    'use strict';

    var MainRouter = Backbone.Router.extend({
        routes: {
          'patients': 'showPatients',
          'patients/add/:registry': 'addPatient',
          'patients/:patientid/evaluations': 'showEvaluations',
          'patients/:patientid/edit': 'editPatient',
          'patients/:patientid/evaluations/:evaluationid': 'showPatientEvaluation',
          'patients/:patientid/addevaluation': 'addPatientEvaluation',
          'patients/:patientid/predeliric': 'showPatientPredeliric'

        },
      showPatients: function(){
        app.setMainView(new PatientsView({collection: app.data.patients}).render())
      },
      addPatient: function(registry){
        var patient = new PatientModel({registry:registry});
        app.setMainView(new PatienteditView({collection: app.data.patients, model: patient}).render(),'#/patients')
      },
        editPatient: function (patientId) {
            var patient = app.data.patients.get(patientId);
            var view;
            if (patient){
                view = new PatienteditView({model: patient});
                app.setMainView(view.render(), '#/patients');
            } else {
                alert('Paciente com id ' + patientId + ' não encontrado');
                app.mainRouter.navigate('#/patients');
            }
        },
        showEvaluations: function (patientId) {
            var patient = app.data.patients.get(patientId);
            var view;
            if (patient){
                patient.getEvaluations().done(function(evaluations){
                    view = new EvaluationsView({model: patient, evaluations: evaluations});
                    app.setMainView(view.render(), '#/patients');
                });
            } else {
                alert('Paciente com id ' + patientId + ' não encontrado');
                app.mainRouter.navigate('#/patients');
            }

        },
        showPatientEvaluation: function (patientId, evaluationId) {
            var patient = app.data.patients.get(patientId);
            var evaluation;
            var view;
            if (patient) {
                patient.getEvaluations().done(function (evaluations) {
                      evaluation = evaluations.get(evaluationId);
                      if (evaluation) {
                          view = new EvaluationView({model: evaluation, collection: evaluations});
                          app.setMainView(view.render(), '#/patients/' + patientId + '/evaluations');
                      }
                }).fail(function () {
                    alert('Evolução com id ' + evaluationId + ' não encontrada');
                });
            }

        },
        addPatientEvaluation: function(patientId){
            var patient = app.data.patients.get(patientId);
            var view;
            var evaluation = new Evaluation({});
            patient.getEvaluations().done(function (evaluations) {
                view = new EvaluationView({model: evaluation, collection: evaluations});
                app.setMainView(view.render(), '#/patients/' +patientId +'/evaluations');
            });
        },
        showPatientPredeliric: function (patientId) {
            var patient = app.data.patients.get(patientId);
            var view;
            if (patient) {
                patient.getPreDeliric().done(function (predeliric) {
                    view = new PreDeliricView({model: predeliric});
                    app.setMainView(view.render(), '#/patients/' + patientId + '/evaluations');
                }).fail(function () {
                    alert('Predeliric do paciente com id ' + patientId + ' não encontrada');
                });
            }
        }
    });
    return MainRouter;
});
