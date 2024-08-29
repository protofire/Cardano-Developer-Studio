import {
  AppStateContext,
} from "@/pages/_app";
import {
  safeStringToBigInt,
  signAndSubmitTx,
} from "@/utilities/utilities";
import {
  fromText,
  getAddressDetails,
  MintingPolicy,
  PolicyId,
  Unit,
  UTxO,
} from "lucid-cardano";
import { applyParamsToScript, Data } from "lucid-cardano";
import { useContext, useState } from "react";

export default function Stablecoin() {
  const { appState, setAppState } = useContext(AppStateContext);
  const {
    lucid,
    wAddr,
    contractType,
    nftTokenNameHex,
    nftPolicy,
    nftAssetClassHex,
  } = appState;
  const [amount, setAmount] = useState(1n);

  const Params = Data.Tuple([Data.Bytes(), Data.Integer()]);
  type Params = Data.Static<typeof Params>;

  type GetFinalPolicy = {
    policyScript: MintingPolicy;
    unit: Unit;
  };

  const redeemer = Data.Enum([Data.Literal("Mint"), Data.Literal("Burn")]);
  type Redeemer = Data.Static<typeof redeemer>;

  ///////////////////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////// HELPER FUNCTIONS ///////////////////////////////////////////
  const getFinalPolicy = async (utxo: UTxO): Promise<GetFinalPolicy> => {
    const tn = fromText(nftTokenNameHex!);
    const policyScript: MintingPolicy = {
      type: "PlutusV2",
      script: applyParamsToScript<Params>(
        "5907c85907c5010000323232323232323232323232323232323232323232323232323232323232323232323232322232223232325335533553335734605400203a2a666ae68c0a400407c09c4c94cd4d5400c888888888888ccd5cd19b873301700800d3370290002400405605a03c266ae712411357726f6e6720616d6f756e74206275726e656400021300e500315335533535500222222222222233355302f0303302d2233301703300200130150322325333573466e3cd40040ccd40500cc4ccd5cd19b87350010343501403402c02e02d3500103200c01d1335738921115554784f206e6f7420636f6e73756d65640002013253353550032222222222223335734606e6602e01001a05605a03c266ae7124011357726f6e6720616d6f756e74206d696e74656400021300e500301f1120011635573a6ea80104d5400408c4c94ccd5cd18139aab9d00113232323212330010030023018357426ae8800d4ccd5cd18149aab9d00113232323232323232323232323232323232323232323232321233333333333300101801601401201000e00c0090070050030023033357426ae88008ccc0add710009aba10013574400466605205640026ae84004d5d1001198143ae357420026ae8800d4ccd5cd181c9aab9d00113232323232123300100400253335734607a6aae740044c8c8c848cc00400c008c094d5d09aba20023302575a6ae84004d55cf00081d9baa357426ae8800d4ccd5cd181d9aab9d001132323212330010030023023357426ae88008cc08dd69aba100135573c0020726ea8d5d08009aab9e00103737546ae84004d5d10011998110133ad357420026ae88008cc08408cd5d08009aba200233301e75c03a6ae84004d5d100119980e3ae01b357420026ae88008cc06c05cd5d08009aba200233019014357420026ae88008cc05c048d5d08009aab9e00102737546ae84004d55cf0008129baa0013301b375c0046eb40048cc004894cd400805c400406088cccd54c06c070cc06488ccd401c07c004008d401407888cdc00008011802091199802911a8011112999ab9a3371e00601026600e002008200800264600e244666010446a00444666018446a00444a666ae68c0b8004400c4cc02848ccc00401c00c00800c00c00400400cc018488ccc01c88d400888cc018cc08400800400c00401920002230242253350011003221330060023004001233500101701333014222300330020012001235001223333500101d200101d01d23253335734603a0020202a666ae68c070004048068d55ce9baa00122323253335734603e002224440022a666ae68c0780044c84888c00c010c010d5d09aab9e002153335734603a002224440040366aae74004dd50009192999ab9a301a35573a00226464642466002006004600a6ae84d5d100118069aba100135573c0020306ea80048c94ccd5cd180c9aab9d0011323232323232323232321233330010090070030023300c75c6ae84d5d10022999ab9a302300113212223002004357426aae7800854ccd5cd1811000899091118008021bae357426aae7800854ccd5cd18108008891100180f9aab9d00137546ae84004d5d1001199804bae008357420026ae8800d4ccd5cd180d9aab9d001132323212330010030023300700d357426ae88008c034d5d08009aab9e00101937546ae84004d55cf00080b9baa001223232533357346034002264244600400660086ae84d55cf0010a999ab9a301b00101001835573a0026ea8004cc005d73ad2223301b2233335573e002401e4646602866026600e6aae74004c018d55cf00098021aba2003357420040286eac00488cc06488cccd55cf800900691980898029aba10023003357440040246eb00048c8c94ccd5cd180c00089909111180200298021aba135573c0042a666ae68c05c0044c848888c008014c014d5d09aab9e002153335734602c00226424444600200a600e6ae84d55cf0010a999ab9a3015001132122223003005375c6ae84d55cf0010099aab9d00137540024646464a666ae68cdc3a40180042244444440062a666ae68cdc3a40140042244444440082a666ae68cdc3a40100042646424444444660020120106eb4d5d09aba25002375c6ae85400454ccd5cd180c0010991909111111198010048041bae357426ae894008dd71aba15001153335734602e00426464244444446600c0120106eb8d5d09aba25002300535742a0022a666ae68c0580084c848888888c01c020c014d5d09aab9e003153335734602a00426424444444600a010600a6ae84d55cf00180989aab9e00235573a0026ea80048c8c94ccd5cd180a000899191919190911998008030020019bad357426ae88008dd69aba1001357440046eb4d5d08009aab9e0021533357346026002264244600400660086ae84d55cf0010089aab9d001375400246464a666ae68c04c0044c8488c00400cdd71aba135573c0042a666ae68c0480044c8488c00800cdd71aba135573c0040206aae74004dd50009192999ab9a301035573a0022646601260086ae84004dd69aba1357440026aae78004038dd50009192999ab9a300f35573a00226eb8d5d09aab9e00100d37540022002200e2002200c442466002006004442446600200800660144422444a66a00226a006010442666a00a0126008004666aa600e01000a0080022400244004440022a66ae712401035054310016253357389201024c680016370e90001b8748008dc3a40086e1d200623230010012233003300200200101",
        [utxo.txHash, BigInt(utxo.outputIndex)],
        Params,
      ),
    };
    const policyId: PolicyId = lucid!.utils.mintingPolicyToId(policyScript);
    const unit: Unit = policyId + tn;
    setAppState({
      ...appState,
      nftPolicyIdHex: policyId,
      nftTokenNameHex: tn,
      nftAssetClassHex: unit,
      nftPolicy: policyScript,
    });

    return { policyScript, unit };
  };

  const getUtxo = async (address: string): Promise<UTxO> => {
    const utxos = await lucid!.utxosAt(address);
    const utxo = utxos[0];
    return utxo;
  };
  ///////////////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////// MINT /////////////////////////////////////////////////////

  const mintSC = async () => {
    console.log("mintSC -> appState: ", appState);
    const utxo = await getUtxo(wAddr!);
    const { policyScript: nftPolicy, unit: nftAssetClassHex } =
      await getFinalPolicy(utxo);

    if (!wAddr || !lucid || amount < 0n) return;
    const pkh: string = getAddressDetails(wAddr!).paymentCredential?.hash || "";

    const tx = await lucid!
      .newTx()
      .mintAssets(
        { [nftAssetClassHex]: amount },
        Data.to<Redeemer>("Mint", redeemer),
      )
      .attachMintingPolicy(nftPolicy)
      .addSignerKey(pkh)
      .complete();

    await signAndSubmitTx(tx);
  };

  ///////////////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////// BURN /////////////////////////////////////////////////////

  const burnSC = async () => {
    console.log("burnSC -> appState: ", appState);

    const unit = nftAssetClassHex;
    const policyScript = nftPolicy;

    if (!unit || !policyScript) {
      console.log("NFT script not found");
      return;
    }

    if (!wAddr || !lucid || amount < 0n) return;
    const pkh: string = getAddressDetails(wAddr!).paymentCredential?.hash || "";

    const tx = await lucid!
      .newTx()
      .mintAssets({ [unit]: -amount }, Data.to<Redeemer>("Burn", redeemer))
      .attachMintingPolicy(policyScript)
      .addSignerKey(pkh)
      .complete();

    await signAndSubmitTx(tx);
  };

  ///////////////////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////// UI /////////////////////////////////////////////////

  return (
    <div className="text-zinc-800 font-quicksand">
      {contractType == "policy" && (
        <div className="shadow-[0_4px_0px_0px_rgba(0,0,0,0.25)] w-[664px] bg-zinc-50 border border-zinc-600 rounded-xl p-9">
          <div className="w-full flex flex-row gap-4 mt-2">
            <p>Token name:</p>
            <input
              className="w-160 py-1 px-2 ml-2 border border-zinc-700 rounded"
              type="string"
              value={nftTokenNameHex || ""}
              onChange={(e) => {
                const am = String(e.target.value);
                if (!am) return;
                setAppState({
                  ...appState,
                  nftTokenNameHex: am,
                });
              }}
            />
            <p>Tokens amount (units):</p>
            <input
              className="w-16 py-1 px-2 ml-2 border border-zinc-700 rounded"
              type="number"
              value={Number(amount)}
              onChange={(e) => {
                const am = safeStringToBigInt(e.target.value);
                if (!am) return;
                setAmount(am);
              }}
            />
          </div>
          <div className="w-full flex flex-row justify-center gap-4 mt-2">
            <button
              onClick={mintSC}
              disabled={!lucid || !wAddr || !amount || !nftTokenNameHex}
              className="w-full rounded-lg p-3 text-zinc-50 bg-zinc-800 shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)] disabled:active:translate-y-0 disabled:active:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:bg-zinc-200  disabled:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:text-zinc-600 font-quicksand font-bold active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)]"
            >
              {" "}
              Mint Tokens
            </button>
            <button
              onClick={burnSC}
              disabled={!lucid || !wAddr || !amount || !nftTokenNameHex || !nftPolicy}
              className="w-full rounded-lg p-3 text-zinc-50 bg-zinc-800 shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)] disabled:active:translate-y-0 disabled:active:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:bg-zinc-200  disabled:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:text-zinc-600 font-quicksand font-bold active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)]"
            >
              {" "}
              Burn Tokens
            </button>
          </div>
        </div>
      )}
    </div>
  );
}
