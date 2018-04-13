/*
 * This file depends on dic.js
 */

var forcedLanguage;
var dic;
var DEFAULT_LANGUAGE = 'pt';
var fallbackDic = ['pt', 'common'];

if (!dic) {
    var dependence = 'i18n.js depends on dic.js';
    console.error(dependence);
    throw new Error(dependence);
}

function getUrlParameter(name) {
    name = name.replace(/[\[]/, "\\\[").replace(/[\]]/, "\\\]");
    var regexS = "[\\?&]" + name + "=([^&#]*)";
    var regex = new RegExp(regexS);
    var results = regex.exec(window.location.href);
    if (results == null) return null;
    else return results[1];
}

function getLanguageFromBrowser() {
    return navigator.language ? navigator.language : DEFAULT_LANGUAGE;
}

function setNumeralLanguage(lang) {
    if (lang == 'pt') {
        lang = 'pt-br';
    }

    numeral.language(lang);
}

function setMomentLanguage(lang) {
    if (lang == 'pt') {
        lang = 'pt-br';
    }

    moment.locale(lang);
}

function setToolsLanguage(lang) {
    if (lang) {
        lang = lang.toLowerCase();
    }

    try {
        numeral.language(lang);
        wt.log('Language format set to -> ' + lang + ' (1)');
    } catch (err) {
        var simplifiedLanguage = getSimplifiedLanguage();

        try {
            setNumeralLanguage(simplifiedLanguage);
            wt.log('Language format set to -> ' + simplifiedLanguage + ' (2)');
        } catch (err) {
            setNumeralLanguage(DEFAULT_LANGUAGE);
            wt.log('Language format set to -> ' + DEFAULT_LANGUAGE + ' (default / 3)');
        }

        document.documentElement.lang = simplifiedLanguage;
    }

    setMomentLanguage(DEFAULT_LANGUAGE);
    var dateFormatLanguageSet = moment.locale(lang);
    wt.log('Date format set to -> ' + dateFormatLanguageSet);
}

function discoverAndGetLanguage() {
    var lang = getUrlParameter('lang');

    //noinspection SwitchStatementWithNoDefaultBranchJS
    switch (lang) {
        case 'pt':
            lang = 'pt-BR';
            break;

        case 'en':
            lang = 'en-US';
            break;
    }

    if (!lang) {
        lang = wt.storage.get('lang', null, true);

        if (!lang && forcedLanguage && forcedLanguage.indexOf('the_language') < 0) {
            lang = forcedLanguage;

            if (!lang) {
                lang = getLanguageFromBrowser();

                if (!lang) {
                    lang = DEFAULT_LANGUAGE;
                    console.log('Not lucky, setting language as "' + DEFAULT_LANGUAGE + '" by default.');
                } else {
                    console.log('Found language in browser: ' + lang);
                }
            } else {
                console.log('Found language in filename: ' + lang);
            }
        } else {
            console.log('Found language in local storage: ' + lang);
        }
    } else {
        console.log('Found language in parameter: ' + lang);
    }

    wt.storage.set('lang', lang, true);
    return lang;
}

function getSimplifiedLanguage() {
    var language = wt.storage.get('lang', null, true);

    if (language && language.length > 2) {
        language = language.substr(0, 2); // get pt from pt-BR for example
    }

    if (language == '__') {
        language = DEFAULT_LANGUAGE;
    }

    return language;
}

function getMessage(key, replacementsArray) {
    var result;
    var language = getSimplifiedLanguage();
    result = getMessage_engine(key, language, replacementsArray);
    return result;
}

/**
 * Short wrapper for getMessage.
 *
 * @param key The key to get the translation.
 * @param [replacementsArray]
 * @returns {*}
 */
function tr(key, replacementsArray) {
    return getMessage(key, replacementsArray);
}

String.prototype.format = function () {
    var args = arguments;

    return this.replace(/\{(\d+)\}/g, function () {
        return args[arguments[1]];
    });
};

function getMessage_engine(key, language, replacementsArray, doNotFallback) {
    var message = !dic || !dic[language] ? null : dic[language][key];

    if (!doNotFallback && (message === null || message === undefined)) {
        for (var i = 0; i < fallbackDic.length; i++) {
            if (fallbackDic[i] != language) {
                message = getMessage_engine(key, fallbackDic[i], replacementsArray, true);

                if (!(message === null || message === undefined)) {
                    break;
                }
            }
        }
    }

    var notFound = (message === null || message === undefined);

    if (!doNotFallback && notFound) {
        message = '[?i18n? ' + language + ': ' + key + ']';
    }

    if (!replacementsArray || notFound) {
        return message;
    } else {
        return message.replace(/\{(\d+)\}/g, function () {
            return replacementsArray[arguments[1]];
        });
    }
}
"i18n.js";
