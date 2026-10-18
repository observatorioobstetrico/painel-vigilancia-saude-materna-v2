// Navegando para uma aba da sidebar pelo seu data-value (chamada via R ou JS)
var openTab = function(tabName){
  $('a', $('.sidebar')).each(function() {
    if(this.getAttribute('data-value') == tabName) {
      this.click()
    };
  });
}

// =============================================================================
// 1. LAYOUT — ALTURA DA NAVBAR E BARRA DE TÍTULO (.div-titulo)
//    A barra de título usa `top: var(--altura-navbar)` para colar abaixo da
//    navbar fixa; a altura real da navbar é medida aqui. No mobile a barra é
//    `fixed` e o `.div-titulo-spacer` compensa a altura dela no fluxo.
// =============================================================================

var _alturaNavbarCache = 0;

function atualizarAlturaNavbar() {
  var navbar = document.querySelector('.wrapper .main-header');
  if (!navbar) return;
  // ceil (não offsetHeight, que trunca): fração de px deixava fresta de
  // subpixel entre a navbar e a barra com o conteúdo rolando por trás.
  var h = Math.ceil(navbar.getBoundingClientRect().height);
  if (h === _alturaNavbarCache) return;
  _alturaNavbarCache = h;
  document.documentElement.style.setProperty('--altura-navbar', h + 'px');
}

var _ultimaAlturaTitulo = null;

function atualizarAlturaSpacer() {
  var barra = document.querySelector('.div-titulo');
  var spacer = document.getElementById('div-titulo-spacer');
  if (!barra || !spacer) return;
  var h = 0;
  if (window.innerWidth <= 991.98) {
    // offsetHeight exclui a margin-bottom da barra (18px, fora do fluxo no
    // fixed): soma-se a margem para o spacer preservar o mesmo respiro.
    var margem = parseFloat(window.getComputedStyle(barra).marginBottom) || 0;
    h = barra.offsetHeight + margem;
  }
  if (h === _ultimaAlturaTitulo) return;
  _ultimaAlturaTitulo = h;
  document.documentElement.style.setProperty('--titulo-height', h + 'px');
  spacer.style.height = h > 0 ? h + 'px' : '0px';
}

function atualizarLayout() {
  atualizarAlturaNavbar();
  atualizarAlturaSpacer();
}

window.addEventListener('load', function() {
  atualizarLayout();

  // A barra é atualizada por conditionalPanel; observar mudanças de tamanho
  // dela mantém o spacer sincronizado quando o título quebra de linha em
  // trocas de aba. (Registrado no load: no <head> o DOM ainda não existe.)
  if (window.ResizeObserver) {
    var barra = document.querySelector('.div-titulo');
    if (barra) {
      var _observadorTitulo = new ResizeObserver(function() {
        atualizarAlturaSpacer();
      });
      _observadorTitulo.observe(barra);
    }
  }
});

window.addEventListener('resize', atualizarLayout);

// Troca de aba: o título visível muda (inclusive de altura, quando quebra em
// mais linhas) — reapresenta a barra e remede o spacer.
$(document).on('shiny:inputchanged', function(event) {
  if (event.name === 'abas') {
    var barra = document.querySelector('.div-titulo');
    if (barra) barra.classList.remove('nav-scroll-hidden');
    setTimeout(atualizarLayout, 0);
    setTimeout(atualizarLayout, 300);
  }
});

// =============================================================================
// 2. MOBILE — AUTO-HIDE DA BARRA DE TÍTULO
//    Em telas <= 991.98px, oculta a barra ao rolar para baixo e restaura ao
//    rolar para cima (o spacer mantém o espaço reservado no fluxo).
// =============================================================================

(function() {
  var ultimoScrollY = 0;
  var agendado = false;

  function atualizarVisibilidadeBarra() {
    var barra = document.querySelector('.div-titulo');
    if (!barra) { agendado = false; return; }

    if (window.innerWidth > 991.98) {
      barra.classList.remove('nav-scroll-hidden');
      agendado = false;
      return;
    }

    var atual = window.scrollY;
    if (atual > ultimoScrollY && atual > 80) {
      barra.classList.add('nav-scroll-hidden');
    } else {
      barra.classList.remove('nav-scroll-hidden');
    }
    ultimoScrollY = atual;
    agendado = false;
  }

  window.addEventListener('scroll', function() {
    if (!agendado) {
      requestAnimationFrame(atualizarVisibilidadeBarra);
      agendado = true;
    }
  }, { passive: true });

  window.addEventListener('resize', function() {
    var barra = document.querySelector('.div-titulo');
    if (barra && window.innerWidth > 991.98) {
      barra.classList.remove('nav-scroll-hidden');
    }
  });
})();

// =============================================================================
// 3. MOBILE — SIDEBAR (DRAWER)
//    Ao escolher uma aba no menu em telas pequenas, a sidebar se fecha para
//    dar lugar ao conteúdo (o AdminLTE só fecha pelo overlay ou pelo
//    hamburger). Só links folha (com data-value) disparam o fechamento —
//    itens de grupo (treeview) continuam expandindo normalmente.
// =============================================================================

$(document).on('click', '.main-sidebar a[data-value]', function() {
  if (window.innerWidth <= 991.98 && document.body.classList.contains('sidebar-open')) {
    $('[data-widget="pushmenu"]').trigger('click');
  }
});

// Cabeçalho do drawer: botão de fechar (saída explícita além do hamburger e
// do overlay) e rolagem até a seção ativa ao abrir (orientação).
$(document).ready(function() {
  var brandLink = document.querySelector('.main-sidebar .brand-link');
  if (brandLink && !brandLink.querySelector('.btn-sidebar-fechar')) {
    var btn = document.createElement('button');
    btn.type = 'button';
    btn.className = 'btn-sidebar-fechar';
    btn.setAttribute('aria-label', 'Fechar menu');
    btn.innerHTML = '<i class="fas fa-times" aria-hidden="true"></i>';
    btn.addEventListener('click', function(ev) {
      ev.preventDefault();
      ev.stopPropagation();
      $('[data-widget="pushmenu"]').trigger('click');
    });
    brandLink.appendChild(btn);
  }
});

// Ao abrir o drawer, rola o menu até a seção ativa (orientação)
$(document).on('click', '[data-widget="pushmenu"]', function() {
  if (window.innerWidth <= 991.98) {
    setTimeout(function() {
      if (!document.body.classList.contains('sidebar-open')) return;
      var ativo = document.querySelector('.main-sidebar .nav-link.active');
      if (ativo && ativo.scrollIntoView) {
        ativo.scrollIntoView({ block: 'center', behavior: 'smooth' });
      }
    }, 400);
  }
});

// =============================================================================
// 4. MOBILE — PAINEL DE FILTROS DESLIZANTE
//    Os filtros globais viram um painel da direita, aberto pelo botão
//    flutuante "Filtros" e fechado pelo X, pelo backdrop ou ao buscar.
// =============================================================================

function fecharFiltrosMobile() {
  document.body.classList.remove('filtro-mobile-aberto');
}

$(document).on('click', '#btn-filtros-fab', function() {
  document.body.classList.add('filtro-mobile-aberto');
});

$(document).on('click', '#btn-filtros-fechar, .filtros-mobile-backdrop', function() {
  fecharFiltrosMobile();
});

// Ao buscar, fecha o painel para revelar os resultados (pequeno atraso para o
// clique chegar ao Shiny antes do fechamento).
$(document).on('click', '#pesquisar', function() {
  if (window.innerWidth <= 991.98) {
    setTimeout(fecharFiltrosMobile, 150);
  }
});

// Trocar de aba fecha o painel e a sidebar
$(document).on('shiny:inputchanged', function(event) {
  if (event.name === 'abas') {
    fecharFiltrosMobile();
  }
});

// Tecla ESC fecha o painel de filtros e o menu lateral
$(document).on('keydown', function(event) {
  if (event.key === 'Escape') {
    fecharFiltrosMobile();
    if (window.innerWidth <= 991.98 && document.body.classList.contains('sidebar-open')) {
      $('[data-widget="pushmenu"]').trigger('click');
    }
  }
});
