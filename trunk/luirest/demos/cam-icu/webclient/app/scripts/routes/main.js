/*global define*/

define([
    'jquery',
    'backbone',
    'models/patient',
    'views/patients',
    'views/patientedit',
    'views/evaluations',
    'views/evaluation',
    'models/evaluation'
], function ($, Backbone, PatientModel, PatientsView, PatienteditView, EvaluationsView, EvaluationView, Evaluation) {
    'use strict';

    var MainRouter = Backbone.Router.extend({
        routes: {
          'patients': 'showPatients',
          'patients/add/:registry': 'addPatient',
          'patients/:patientid/evaluations': 'showEvaluations',
          'patients/:patientid/edit': 'editPatient',
          'patients/:patientid/evaluations/:evaluationid': 'showPatientEvaluation',
          'patients/:patientid/addevaluation': 'addPatientEvaluation'
        },
      showPatients: function(){
        app.setMainView(new PatientsView({collection: app.data.patients}).render())
      },
      addPatient: function(registry){
        var patient = new PatientModel({registry:registry});
        app.setMainView(new PatienteditView({collection: app.data.patients, model: patient}).render())
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
        },
        showEvaluations: function (patientId) {
            var patient = app.data.patients.get(patientId);
            var view;
            if (patient){
                patient.getEvaluations(function(evaluations){
                    view = new EvaluationsView({model: patient, evaluations: evaluations});
                    app.setMainView(view.render());
                });
            } else {
                alert('Paciente com id ' + patientId + ' não encontrado');
                app.mainRouter.navigate('#/patients');
            }

        },
        showPatientEvaluation: function (patientId, evaluationId) {
            var patient = app.data.patients.get(patientId);
            var evaluation, evaluations;
            if (patient) {
                evaluations = patient.getEvaluations();
                evaluation = evaluations.get(evaluationId);
            }
            var view;
            if (evaluation){
                view = new EvaluationView({model: evaluation, collection: evaluations});
                app.setMainView(view.render());
            } else {
                alert('Evolução com id ' + evaluationId + ' não encontrada');
            }

        },
        addPatientEvaluation: function(patientId){
            var patient = app.data.patients.get(patientId);
            var evaluations = patient.getEvaluations();
            var evaluation = new Evaluation({});
            var view = new EvaluationView({model: evaluation, collection: evaluations});
            app.setMainView(view.render());
        }

    });

    return MainRouter;
});
