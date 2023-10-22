$(document).on("shiny:inputchanged", function atualizarAltura() {
  var divSuperior = document.querySelector(".tab-pane.container-fluid.active").getElementsByTagName("div")[0];
  var divDependente = document.querySelector(".div-dependente");

  divDependente.style.height = divSuperior.clientHeight + "px";
});


var openTab = function(tabName){
  $('a', $('.sidebar')).each(function() {
    if(this.getAttribute('data-value') == tabName) {
      this.click()
    };
  });
}
