function disableHighlight() {
    const style = document.createElement('style');
    style.textContent = `
        mark, span.mark {
            background: none;
        }`;
    document.head.append(style);
    console.log('highlight disabled')
}

function toggleDarkMode() {
    var body = document.body;
    body.classList.toggle("dark-mode");
    var isDarkNow = body.className == "dark-mode"
    document.cookie = 'theme=' + (isDarkNow ? 'dark' : 'light') + ';samesite=lax; path=/'
    console.log('set ', document.cookie)
}

function defaultMode(){
    // https://stackoverflow.com/a/57795495/6212301
    if (window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches) {
        var body = document.body;
        body.className = "dark-mode";
    }
}

function setThemeFromCookie() {
    var body = document.body;
    if (document.cookie.match(/theme=/i) == null){
        defaultMode()
    } else{
        var isDarkThemeSelected = document.cookie.match(/theme=dark/i) != null;
        body.className = isDarkThemeSelected ? 'dark-mode' : '';
    }
}
  
(function() {
    setThemeFromCookie()
})();
  