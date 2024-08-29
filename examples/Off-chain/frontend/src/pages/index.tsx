import { useContext } from "react";
import {
  AppStateContext,
  ContractClass,
  ContractType,
} from "./_app";
import { ExplorerLink } from "@/components/ExplorerLinks";
import AlwaysContract from "@/components/AlwaysContract";
import CheckDateContract from "@/components/CheckDateContract";
import CheckSignatureContract from "@/components/CheckSignatureContract";
import FTPolicy from "@/components/FTPolicy";
import NFTPolicy from "@/components/NFTPolicy";
import { HiUserCircle } from "react-icons/hi";
import { IoReloadCircleSharp } from "react-icons/io5";

export default function Home() {
  const { appState, setAppState } = useContext(AppStateContext);
  const {
    wAddr,
    contractType,
    contractClass,
    alwaysTrueAddress,
    alwaysFalseAddress,
    checkDateAfterAddress,
    checkDateBeforeAddress,
    checkSignatureAddress,
  } = appState;

  const refreshWallet = async () => {
    if (!appState.lucid || !window.cardano.nami) return;
    const nami = await window.cardano.nami.enable();
    appState.lucid.selectWallet(nami);
    setAppState({
      ...appState,
      wAddr: await appState.lucid.wallet.address(),
    });
  };

  const handleClickContractType = (v: ContractType) => {
    setAppState({
      ...appState,
      contractType: v,
      contractClass: "alwaysTrue",
    });
    console.log(contractType);
  };
  const handleClickContractClass = (v: ContractClass) => {
    setAppState({
      ...appState,
      contractClass: v,
    });

    console.log(contractClass);
  };
  return (
    <main className="flex min-h-screen w-screen h-screen gap-6 flex-row-reverse items-center justify-between px-5 pb-5  pt-20 bg-zinc-800">
      <div className="flex flex-col items-center justify-start  w-[380px] mt-2">
        {/* USER LOGGED */}
        <div className="absolute justify-center items-center right-0 top-5 bg-zinc-50  h-12  w-48 rounded-l-2xl flex flex-row">
          <HiUserCircle
            className="text-4xl text-zinc-600"
            onClick={refreshWallet}
          />
          <p className="text-lg mx-2 text-zinc-800">
            {wAddr ? `...${wAddr.substring(102)}` : ""}
          </p>
          <IoReloadCircleSharp
            className="text-3xl mx-2 text-zinc-600 active:text-zinc-800"
            onClick={refreshWallet}
          />
        </div>

        {/* INFORMATION TABLE */}
        <p className=" overflow-clip self-start tracking-[0.2em]  text-xs text-zinc-200">
          INFO TABLE
        </p>

        <div className=" overflow-hidden bg-zinc-50 rounded-lg w-full my-4 h-auto border border-spacing-1 border-zinc-50">
          <ExplorerLink message="Wallet: " type="address" value={wAddr || ""} />
          <ExplorerLink
            message="Always true validator address: "
            value={alwaysTrueAddress || ""}
            type="address"
          />
          <ExplorerLink
            message="Always false validator address: "
            type="address"
            value={alwaysFalseAddress || ""}
          />
          <ExplorerLink
            message="Check date after validator address: "
            type="address"
            value={checkDateAfterAddress || ""}
          />
          <ExplorerLink
            message="Check date before validator address: "
            type="address"
            value={checkDateBeforeAddress || ""}
          />
          <ExplorerLink
            message="Check Signature validator address: "
            type="address"
            value={checkSignatureAddress || ""}
          />
        </div>
      </div>

      {/* ContractType BUTTONS */}
      {contractType == "undefined" && (
        <div className="absolute top-4 left-5 flex flex-row gap-4">
          <button
            onClick={() => handleClickContractType("undefined")}
            className={`${"bg-zinc-100 text-zinc-800 shadow-[0_5px_0px_0px_rgba(255,251,251,0.6)]"}  font-quicksand text-lg font-bold py-3 px-8 rounded-lg active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)] `}
          >
            🏠
          </button>
          <button
            onClick={() => handleClickContractType("validator")}
            className={`${"bg-zinc-900 text-zinc-50 shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)]"}  font-quicksand text-lg font-bold py-3 px-8 rounded-lg active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)] `}
          >
            Validators
          </button>
          <button
            onClick={() => handleClickContractType("policy")}
            className={`${"bg-zinc-900 text-zinc-50 shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)]"}  font-quicksand text-lg font-bold py-3 px-8 rounded-lg active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)] `}
          >
            Policies
          </button>
        </div>
      )}
      {contractType == "validator" && (
        <div className="absolute top-4 left-5 flex flex-row gap-4">
          <button
            onClick={() => handleClickContractType("undefined")}
            className={`${"bg-zinc-900 text-zinc-50 shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)]"}  font-quicksand text-lg font-bold py-3 px-8 rounded-lg active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)] `}
          >
            ⮜
          </button>
          <button
            onClick={() => handleClickContractClass("alwaysTrue")}
            className={`${
              contractClass == "alwaysTrue"
                ? "bg-zinc-100 text-zinc-800 shadow-[0_5px_0px_0px_rgba(255,251,251,0.6)]"
                : "bg-zinc-900 text-zinc-50 shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)]"
            }  font-quicksand text-lg font-bold py-3 px-8 rounded-lg active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)] `}
          >
            Always true
          </button>
          <button
            onClick={() => handleClickContractClass("alwaysFalse")}
            className={`${
              contractClass == "alwaysFalse"
                ? "bg-zinc-100 text-zinc-800 shadow-[0_5px_0px_0px_rgba(255,251,251,0.6)]"
                : "bg-zinc-900 text-zinc-50 shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)]"
            }  font-quicksand text-lg font-bold py-3 px-8 rounded-lg active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)] `}
          >
            Always false
          </button>
          <button
            onClick={() => handleClickContractClass("checkDate")}
            className={`${
              contractClass == "checkDate"
                ? "bg-zinc-100 text-zinc-800 shadow-[0_5px_0px_0px_rgba(255,251,251,0.6)]"
                : "bg-zinc-900 text-zinc-50 shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)]"
            }  font-quicksand text-lg font-bold py-3 px-8 rounded-lg active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)] `}
          >
            Check date
          </button>
          <button
            onClick={() => handleClickContractClass("checkSignature")}
            className={`${
              contractClass == "checkSignature"
                ? "bg-zinc-100 text-zinc-800 shadow-[0_5px_0px_0px_rgba(255,251,251,0.6)]"
                : "bg-zinc-900 text-zinc-50 shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)]"
            }  font-quicksand text-lg font-bold py-3 px-8 rounded-lg active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)] `}
          >
            Check signature
          </button>
        </div>
      )}
      {contractType == "policy" && (
        <div className="absolute top-4 left-5 flex flex-row gap-4">
          <button
            onClick={() => handleClickContractType("undefined")}
            className={`${"bg-zinc-900 text-zinc-50 shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)]"}  font-quicksand text-lg font-bold py-3 px-8 rounded-lg active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)] `}
          >
            ⮜
          </button>
          <button
            onClick={() => handleClickContractClass("alwaysTrue")}
            className={`${
              contractClass == "alwaysTrue"
                ? "bg-zinc-100 text-zinc-800 shadow-[0_5px_0px_0px_rgba(255,251,251,0.6)]"
                : "bg-zinc-900 text-zinc-50 shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)]"
            }  font-quicksand text-lg font-bold py-3 px-8 rounded-lg active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)] `}
          >
            Always true
          </button>
          <button
            onClick={() => handleClickContractClass("alwaysFalse")}
            className={`${
              contractClass == "alwaysFalse"
                ? "bg-zinc-100 text-zinc-800 shadow-[0_5px_0px_0px_rgba(255,251,251,0.6)]"
                : "bg-zinc-900 text-zinc-50 shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)]"
            }  font-quicksand text-lg font-bold py-3 px-8 rounded-lg active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)] `}
          >
            Always false
          </button>
          <button
            onClick={() => handleClickContractClass("checkDate")}
            className={`${
              contractClass == "checkDate"
                ? "bg-zinc-100 text-zinc-800 shadow-[0_5px_0px_0px_rgba(255,251,251,0.6)]"
                : "bg-zinc-900 text-zinc-50 shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)]"
            }  font-quicksand text-lg font-bold py-3 px-8 rounded-lg active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)] `}
          >
            Check date
          </button>
          <button
            onClick={() => handleClickContractClass("checkSignature")}
            className={`${
              contractClass == "checkSignature"
                ? "bg-zinc-100 text-zinc-800 shadow-[0_5px_0px_0px_rgba(255,251,251,0.6)]"
                : "bg-zinc-900 text-zinc-50 shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)]"
            }  font-quicksand text-lg font-bold py-3 px-8 rounded-lg active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)] `}
          >
            Check signature
          </button>
          <button
            onClick={() => handleClickContractClass("FT")}
            className={`${
              contractClass == "FT"
                ? "bg-zinc-100 text-zinc-800 shadow-[0_5px_0px_0px_rgba(255,251,251,0.6)]"
                : "bg-zinc-900 text-zinc-50 shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)]"
            }  font-quicksand text-lg font-bold py-3 px-8 rounded-lg active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)] `}
          >
            Mint/Burn FT
          </button>
          <button
            onClick={() => handleClickContractClass("NFT")}
            className={`${
              contractClass == "NFT"
                ? "bg-zinc-100 text-zinc-800 shadow-[0_5px_0px_0px_rgba(255,251,251,0.6)]"
                : "bg-zinc-900 text-zinc-50 shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)]"
            }  font-quicksand text-lg font-bold py-3 px-8 rounded-lg active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)] `}
          >
            Mint/Burn NFT
          </button>
        </div>
      )}

      {/* ACTIONS SECTION */}
      <div className="flex flex-col items-center gap-8  h-full py-10 bg-zinc-50 w-4/5 rounded-2xl">
        {/* ORACLE ACTIONS */}
        {/* OWNER ACTIONS */}
        {contractType != "undefined" && (
          (((contractClass == "alwaysTrue") || (contractClass == "alwaysFalse")) && <AlwaysContract />) ||
          ((contractClass == "checkDate")  && <CheckDateContract />) ||
          ((contractClass == "checkSignature")  && <CheckSignatureContract />) ||
          ((contractClass == "FT")  && <FTPolicy/>) ||
          ((contractClass == "NFT")  && <NFTPolicy/>)
        )
        }
      </div>
    </main>
  );
}
