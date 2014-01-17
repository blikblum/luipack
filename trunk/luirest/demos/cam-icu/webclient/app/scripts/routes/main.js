/*global define*/

define([
    'jquery',
    'backbone',
  'views/patients'
], function ($, Backbone, PatientsView) {
    'use strict';

    var MainRouter = Backbone.Router.extend({
        routes: {
          'patients': 'showPatients',
          'patients/add': 'addPatient'
        },
      showPatients: function(){
        app.setMainView(new PatientsView({collection: app.data.patients}).render())
      },
      addPatient: function(){

      }

    });

    return MainRouter;
});
