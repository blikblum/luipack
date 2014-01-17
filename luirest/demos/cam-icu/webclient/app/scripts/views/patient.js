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
        this.listenTo('change', this.render)
        this.listenTo('destroy', this.remove)
      },
      render: function(){
        this.$el.html(this.template(this.model.toJSON()))
        return this
      }

    });

    return PatientView;
});
