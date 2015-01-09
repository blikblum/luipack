/*global define*/

define([
  'jquery',
  'underscore',
  'backbone',
  'templates'
], function ($, _, Backbone, JST) {
  'use strict';

  var PatientView = Backbone.View.extend({
    tagName: 'tr',
    template: JST['app/scripts/templates/patient.hbs'],
    initialize: function () {
      this.listenTo(this.model, 'destroy', this.remove)
    },
    render: function () {
      var context = this.model.toJSON({
        computedFields: true
      });
      this.$el.html(this.template(context))
      return this
    }


  });

  return PatientView;
});