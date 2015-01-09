/*global define*/

define([
  'jquery',
  'underscore',
  'backbone',
  'templates'
], function ($, _, Backbone, JST) {
  'use strict';

  var EvaluationlistitemView = Backbone.View.extend({
    template: JST['app/scripts/templates/evaluationlistitem.hbs'],

    tagName: 'tr',

    id: '',

    className: '',

    events: {},

    initialize: function () {
      this.listenTo(this.model, 'change', this.render);
    },

    render: function () {
      this.$el.html(this.template(this.model.toJSON()));
      return this;
    }
  });

  return EvaluationlistitemView;
});