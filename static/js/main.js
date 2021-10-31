const toggleSidebar = () => {
    let sidebar = document.querySelector(".sidebar-overlay");
    sidebar.classList.toggle("-translate-x-full");
}

const toggleMobileMenu = () => {
    document.querySelector('#mobile-menu').classList.toggle('hidden');
}

(function() {
    var nav = document.querySelector('header>div>nav'),
        anchor = nav.getElementsByTagName('a'),
        current = window.location.pathname.split('/')[1];
        for (var i = 0; i < anchor.length; i++) {
        if(anchor[i].href == current) {
            anchor[i].classList.toggle("navlink-active");
        }
    }
})();
