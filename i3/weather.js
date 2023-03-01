const https = require('https');
const key  = "";
// const location = process.argv[3].startsWith('192.168.178') ? 'Coburg' : 'Ilmenau';
const location = 'Ilmenau';
const uri  = `https://api.openweathermap.org/data/2.5/weather?q=${location}&appid=${key}&units=metric`;


// const output_map = new Map([
//     // rain descriptions
//     ['light rain', 'rain'],
//     ['moderate rain', 'rain'],
//     ['heavy intensity rain','rain'],
//     ['very heavy rain', 'rain'],
//     ['extreme rain,', 'rain'],
//     ['freezing rain', 'rain'],
//     ['light intensity shower rain', 'rain'],
//     ['shower rain', 'rain'],
//     ['heavy intensity shower rain', 'rain'],
//     ['ragged shower rain', 'rain'],
//     // snow descriptions
//     ['light snow','snow'],
//     ['snow','snow'],
//     ['heavy snow','snow'],
//     ['sleet','snow'],
//     ['light shower sleet','snow'],
//     ['shower sleet','snow'],
//     ['light rain and snow','snow'],
//     ['rain and snow','snow'],
//     ['light shower snow', 'snow'],
//     ['shower snow', 'snow'],
//     ['heavy shower snow', 'snow']
// ]);

const output_map = new Map([
    ['Thunderstorm', '🌩️ Гроза'],
    ['Drizzle',      '🌧️ Морось'],
    ['Rain',         '🌧️ Дождь'],
    ['Snow',         '❄️ Снег'],
    ['Mist',         '🌫️ Туман'],
    ['Fog',          '🌫️ Туман'],
    ['Clear',        '☀️ Чистый'],
    ['Clouds',       '☁️ Облака'],
]);

const request = https.request(uri, (response) => {
    let data = '';
    response.on('data', (chunk) => {
        data = data + chunk.toString();
    });

    response.on('end', () => {
        const body = JSON.parse(data);
        let temp = parseFloat(body.main.temp).toFixed(1);
        temp = temp === 0 ? 0 : temp;
        console.log(
            output_map.get(body.weather[0].main)
                + " "
                + temp
                + "°C"
        );
    });
})

request.on('error', (error) => {
    console.error(error);
});

request.end();
