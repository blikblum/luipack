/*global define*/

define([
    'jquery',
    'underscore',
    'backbone',
    'text!templates/evaluations.html',
    'views/evaluationlistitem'
], function ($, _, Backbone, html, EvaluationListItemView) {
    'use strict';

    var EvaluationsView = Backbone.View.extend({
        html: html,

        tagName: 'div',

        id: '',

        className: '',

        events: {},

        bindings: {
            '.name-el':'name',
            '.add-evaluation-el': {
                attributes: [{
                    name: 'href',
                    observe: 'id',
                    onGet: function(val){
                      return '#patients/' + val + '/addevaluation';
                    }
                }]
            }
        },

        initialize: function (options) {
          this.evaluations = options.evaluations;
          this.listenTo(this.evaluations, 'add', this.renderEvaluation);
        },

        render: function () {
            this.$el.html(this.html);
            this.stickit();
            this.$tbody = this.$('#evaluations-table tbody');
            this.evaluations.each(this.renderEvaluation, this);
            return this;
        },
        renderEvaluation: function(evaluation){
          this.$tbody.append(new EvaluationListItemView({model: evaluation}).render().$el)
        }
    });

    return EvaluationsView;
});
