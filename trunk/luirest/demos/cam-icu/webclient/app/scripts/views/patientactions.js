/*global define*/

define([
  'jquery',
  'underscore',
  'backbone',
  'text!templates/patientactions.html'
], function ($, _, Backbone, html) {
  'use strict';

  var PatientActionsView = Backbone.View.extend({
    html: html,

    tagName: 'div',

    bindings: {
      '.name-el': 'name'
    },

    events: {
      'click .edit-cadastre': 'showCadastreView',
      'click .change-bednumber': 'showBednumberView',
      'click .register-exit': 'showDischargeView'
    },

    initialize: function (options) {

    },

    render: function () {
      this.$el.html(this.html);
      this.stickit();
      return this;
    },

    showCadastreView: function (e) {
      e.preventDefault();
      app.mainRouter.navigate('#/patients/' + this.model.get('id') + '/edit', true);
    },
    showBednumberView: function (e) {
      e.preventDefault();
      app.mainRouter.navigate('#/patients/' + this.model.get('id') + '/bednumber', true);
    },
    showDischargeView: function (e) {
      e.preventDefault();
      app.mainRouter.navigate('#/patients/' + this.model.get('id') + '/discharge', true);
    }
  });

  return PatientActionsView;
});