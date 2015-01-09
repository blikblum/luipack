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

    events: {
      'click .add-evaluation-el': 'addEvaluation',
      'click .alert .close': 'closeAlert',
      'click li.predeliric-risk': 'showPreDeliric'
    },

    bindings: {
      '.name-el': 'name',
      '.predeliric-risk .badge': {
        observe: 'predeliricrisk',
        onGet: function (val) {
          if (!val || !isFinite(val)) {
            return 'Pendente'
          } else {
            return val.toFixed(1) + '%'
          }

        },
        attributes: [
          {
            name: 'class',
            observe: 'predeliricrisk',
            onGet: function (val) {
              var classValue = 'badge pull-right';
              if (val && isFinite(val)) {
                if (val >= 50) {
                  classValue += ' badge-error';
                } else {
                  classValue += ' badge-success';
                }
              }
              return classValue;
            }
                    }
                ]
      },
      '.predeliric-risk a': {
        attributes: [{
          name: 'href',
          observe: 'id',
          onGet: function (val) {
            return '#patients/' + val + '/predeliric';
          }
                }]
      }
    },

    initialize: function (options) {
      this.evaluations = options.evaluations;
      this.subViews = [];
      this.listenTo(this.evaluations, 'add', this.renderEvaluation);
    },

    render: function () {
      this.$el.html(this.html);
      this.stickit();
      this.$tbody = this.$('#evaluations-table tbody');
      this.clearSubViews();
      this.evaluations.each(this.renderEvaluation, this);
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
    renderEvaluation: function (evaluation) {
      var view = new EvaluationListItemView({
        model: evaluation
      });
      this.subViews.push(view);
      this.$tbody.append(view.render().$el)
    },
    addEvaluation: function (e) {
      var todayCount;
      var today = Math.floor(toOADate(new Date()));
      e.preventDefault();
      todayCount = this.evaluations.filter(function (model) {
        return Math.floor(model.get('date')) === today;
      }).length;
      if (todayCount > 1) {
        this.$('.alert-duplicate').removeClass('hidden');
      } else {
        app.mainRouter.navigate('#patients/' + this.model.get('id') + '/addevaluation', true);
      }
    },
    closeAlert: function (e) {
      e.preventDefault()
      $(e.currentTarget).parent().addClass('hidden');
    },
    showPreDeliric: function (e) {
      e.preventDefault();
      app.mainRouter.navigate('#patients/' + this.model.get('id') + '/predeliric', true);
    }
  });

  return EvaluationsView;
});