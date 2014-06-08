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
      initialize: function(){
        this.listenTo(this.model, 'change', this.render)
        this.listenTo(this.model, 'destroy', this.remove)
      },
      render: function(){
        this.$el.html(this.template(this.model.toJSON()))
        return this
      }

    });

    return PatientView;
});
