/*global define*/

define([
  'jquery',
  'underscore',
  'backbone',
  'templates',
  'views/patient'
], function ($, _, Backbone, JST, PatientView) {
  'use strict';

  var PatientsView = Backbone.View.extend({
    template: JST['app/scripts/templates/patients.hbs'],
    initialize: function () {
      this.listenTo(this.collection, 'add', this.renderPatient);
      this.subViews = [];
    },
    render: function () {
      this.$el.html(this.template());
      this.$tbody = this.$el.find('#patients-table tbody');
      this.$(".add-popover").popover({
        html: true,
        content: function () {
          return $('.add-popover-content').html();
        }
      });
      this.clearSubViews();
      this.collection.each(this.renderPatient, this);
      return this;
    },
    remove: function () {
      return Backbone.View.prototype.remove.apply(this, arguments);
      this.clearSubViews();
    },
    clearSubViews: function () {
      this.subViews.forEach(function (view) {
        view.remove();
      });
      this.subViews = [];
    },
    renderPatient: function (patient) {
      var view = new PatientView({
        model: patient
      });
      this.subViews.push(view);
      this.$tbody.append(view.render().$el)
    },
    events: {
      'submit .popover-content form': 'addPatient'
    },
    addPatient: function (e) {
      var registry = this.$('.popover-content input.patient-registry').val();
      e.preventDefault()
      app.mainRouter.navigate('#patients/add/' + registry, {
        trigger: true
      });
    }

  });

  return PatientsView;
});