import firebase from 'firebase';
import 'firebase/auth';
import 'firebase/database';

import './styles/main.scss';

import { Main } from './Main';

const config = {
  apiKey: "AIzaSyDitNmfTqVu0B-XCpMMynVJ49hqwPQcU0M",
  authDomain: "elm-tv-app.firebaseapp.com",
  databaseURL: "https://elm-tv-app.firebaseio.com",
  storageBucket: "elm-tv-app.appspot.com",
};

firebase.initializeApp(config);

// inject bundled Elm app into div#main
const elmApp = Main.embed( document.getElementById( 'main' ) );

const db = firebase.database();

const ref = db.ref('sessions/1');
ref.on('value', (snapshot) => {
  const { usersById, showId } = snapshot.val();
  const watchers = usersById.map((user, i) => {
    const { season, episode } = user.lastWatchedEpisode;
    return {
      name: user.name,
      id: i,
      lastWatchedEpisode: [ season, episode ]
    };
  });

  elmApp.ports.firebaseSession.send({ watchers, showId: showId.toString() });
});

elmApp.ports.setFirebaseState.subscribe(({ session, watchers }) => {
  const usersRef = db.ref(`sessions/${session}/usersById`);
  watchers.forEach(({ id, lastWatchedEpisode }) => {
    const [ season, episode ] = lastWatchedEpisode;
    usersRef.child(`${id}/lastWatchedEpisode`).set({
      season,
      episode
    });
  });
})
