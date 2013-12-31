app.AppView = Backbone.View.extend({
  el: 'div.container',
  initialize: function(){
    this.mainView = new app.MainView()
  },
  render: function(){
    this.mainView.render().$el.appendTo(this.$el)
  }
})