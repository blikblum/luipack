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
      initialize: function(){
        this.listenTo(this.collection, 'add', this.renderPatient);
      },
      render: function(){
        this.$el.html(this.template());
        this.$tbody = this.$el.find('#patients-table tbody');
        this.collection.each(this.renderPatient, this);
        return this;
      },
      renderPatient: function(patient){
        this.$tbody.append(new PatientView({model: patient}).render().$el)
      }

    });

    return PatientsView;
});
