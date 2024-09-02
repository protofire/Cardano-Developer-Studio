import React, { useState, useEffect } from 'react';
import Image from 'next/image';

export interface Wallet {
  wallet: string;
  name: string;
  icon: string;
  link: string;
  isInstalled: boolean;
  enable?: () => Promise<any>;
}

interface Props {
  onSelect: (wallet: Wallet) => void;
  onClose: () => void;
}

const CARDANO_WALLETS: Wallet[] = [
  {
    wallet: 'eternl',
    name: 'Eternl',
    icon: '/img/wallet/eternl.png',
    link: 'https://chrome.google.com/webstore/detail/eternl/kmhcihpebfmpgmihbkipmjlmmioameka',
    isInstalled: false,
  },
  {
    wallet: 'nami',
    name: 'Nami',
    icon: '/img/wallet/nami.png',
    link: 'https://chrome.google.com/webstore/detail/nami/lpfcbjknijpeeillifnkikgncikgfhdo',
    isInstalled: false,
  },
  {
    wallet: 'flint',
    name: 'Flint',
    icon: '/img/wallet/flint.png',
    link: 'https://chrome.google.com/webstore/detail/flint-wallet/hnhobjmcibchnmglfbldbfabcgaknlkj',
    isInstalled: false,
  },
  {
    wallet: 'yoroi',
    name: 'Yoroi',
    icon: '/img/wallet/yoroi.png',
    link: 'https://chrome.google.com/webstore/detail/yoroi/ffnbelfdoeiohenkjibnmadjiehjhajb',
    isInstalled: false,
  },
  {
    wallet: 'typhon',
    name: 'Typhon',
    icon: '/img/wallet/typhon.png',
    link: 'https://chrome.google.com/webstore/detail/typhon-wallet/kfdniefadaanbjodldohaedphafoffoh',
    isInstalled: false,
  },
  {
    wallet: 'nufi',
    name: 'Nufi',
    icon: '/img/wallet/nufi.png',
    link: 'https://chrome.google.com/webstore/detail/nufi/gpnihlnnodeiiaakbikldcihojploeca',
    isInstalled: false,
  },
];

const WalletSelectorModal: React.FC<Props> = ({ onSelect, onClose }) => {
  const [wallets, setWallets] = useState<Wallet[]>([]);

  useEffect(() => {
    const availableWallets = CARDANO_WALLETS.map(wallet => ({
      ...wallet,
      isInstalled: !!window.cardano?.[wallet.wallet],
      enable: window.cardano?.[wallet.wallet]?.enable
    }));
    setWallets(availableWallets);
  }, []);

  return (
    <div className="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center">
      <div className="bg-zinc-800 p-6 rounded-lg max-w-md w-full">
        <h2 className="text-xl font-bold mb-4 text-white">Select a Wallet</h2>
        <div className="grid grid-cols-2 gap-4">
          {wallets.map((wallet) => (
            <button
              key={wallet.wallet}
              className={`flex items-center justify-start p-3 rounded ${
                wallet.isInstalled 
                  ? 'bg-zinc-700 hover:bg-zinc-600' 
                  : 'bg-zinc-900 hover:bg-zinc-800 opacity-50'
              } text-white`}
              onClick={() => wallet.isInstalled && wallet.enable && onSelect(wallet)}
              disabled={!wallet.isInstalled}
            >
              <Image 
                src={wallet.icon} 
                alt={`${wallet.name} icon`} 
                width={16} 
                height={16} 
                className="mr-2"
              />
              <span>{wallet.name}</span>
              {!wallet.isInstalled && (
                <a 
                  href={wallet.link} 
                  target="_blank" 
                  rel="noopener noreferrer" 
                  className="ml-auto text-xs text-zinc-400 hover:text-zinc-200 transition-colors"
                  onClick={(e) => e.stopPropagation()}
                >
                  Install
                </a>
              )}
            </button>
          ))}
        </div>
        <button
          className="mt-6 px-4 py-2 bg-zinc-700 text-white rounded w-full hover:bg-zinc-600"
          onClick={onClose}
        >
          Cancel
        </button>
      </div>
    </div>
  );
};

export default WalletSelectorModal;