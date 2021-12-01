import * as React from 'react';
import QRCode from 'react-qr-code';

const YoutubeEmbed = ( { code, image } ) => {
  return (
    <div>
      <div className="video-responsive">
        <iframe
            width="750"
            title="Youtube"
            height="422"
            src={"https://www.youtube.com/embed/" + code}
            frameBorder="0"
            allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture"
            allowFullScreen
        ></iframe>
      </div>
      <div className="video-qrcode">
          <a href={"https://youtu.be/" + code}>
            <image src={image} />
            <QRCode value={"https://youtu.be/" + code} />
            </a>
      </div>
    </div>
  );
};

export default YoutubeEmbed;
