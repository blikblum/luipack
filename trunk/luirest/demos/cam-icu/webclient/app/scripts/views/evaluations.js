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

        events: {
            'click .add-evaluation-el': 'addEvaluation',
            'click .alert .close': 'closeAlert'
        },

        bindings: {
            '.name-el':'name',
            '.predeliric-risk .badge': {
                observe: 'predeliricrisk',
                onGet: function (val) {
                    var num;
                    if  (!val || !isFinite(val)){
                        return 'Pendente'
                    } else {
                        num = val;
                        //todo: enable later
                        //return num.toFixed(1) + '%'
                        return null;
                    }

                },
                attributes: [
                    {
                        name: 'class',
                        observe: 'predeliricrisk',
                        onGet: function (val) {
                            var classValue = 'badge pull-right'
                            if  (!val || !isFinite(val)){
                                return classValue + ' badge-error';
                            } else {
                                return classValue;
                            }
                        }
                    }
                ]
            },
            '.predeliric-risk a': {
                attributes: [{
                    name: 'href',
                    observe: 'id',
                    onGet: function(val){
                        return '#patients/' + val + '/predeliric';
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
        },
        addEvaluation: function (e) {
            var today = Math.floor(toOADate(new Date()));
            e.preventDefault();
            if (this.evaluations.some(function(model) {
               return Math.floor(model.get('date')) === today;
            })) {
                this.$('.alert-duplicate').removeClass('hidden');
            } else {
                app.mainRouter.navigate('#patients/' + this.model.get('id') + '/addevaluation', true);
            }
        },
        closeAlert: function (e) {
            e.preventDefault()
            $(e.currentTarget).parent().addClass('hidden');
        }
    });

    return EvaluationsView;
});
